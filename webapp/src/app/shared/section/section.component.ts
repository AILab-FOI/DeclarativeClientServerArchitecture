import { Component, computed, inject, input, signal } from '@angular/core';
import { Sekcija, UserService } from '../../core';
import { ContentCardComponent } from '../content-card/content-card.component';
import { openModal } from '../modal/modal.component';
import { Dialog, DialogRef } from '@angular/cdk/dialog';
import { SectionEditComponent } from '../section-edit/section-edit.component';

@Component({
  selector: 'app-section',
  standalone: true,
  imports: [ContentCardComponent],
  templateUrl: './section.component.html',
  styleUrl: './section.component.scss',
})
export class SectionComponent {
  public sekcija = input.required<Sekcija>();
  public isWorker = input.required<boolean>();
  public user = inject(UserService);
  private dialog = inject(Dialog);
  public sadrzaj = computed(() => {
    return this.sekcija()?.sadrzaj;
  });

  public open = signal(true);

  openEdit(): void {
    let ref = openModal<Sekcija>(this.dialog, {
      data: structuredClone(this.sekcija()),
      tool: { view: SectionEditComponent },
      inputs: {},
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((section: Sekcija) => {
      if (section) {
        console.log(section);
        // TODO: UREĐIVANJE SEKCIJE QUERY
      }
      ref.close();
    });
  }
}
