import { Component, computed, inject, input, signal } from '@angular/core';
import { Sekcija } from '../../core';
import { ContentComponent } from '../content/content.component';
import { openModal } from '../modal/modal.component';
import { Dialog, DialogRef } from '@angular/cdk/dialog';

@Component({
  selector: 'app-section',
  standalone: true,
  imports: [ContentComponent],
  templateUrl: './section.component.html',
  styleUrl: './section.component.scss',
})
export class SectionComponent {
  public sekcija = input.required<Sekcija>();
  public isWorker = input.required<boolean>();
  private dialog = inject(Dialog);
  public sadrzaj = computed(() => {
    return this.sekcija()?.sadrzaj;
  });

  public open = signal(true);

  openEdit(): void {
    let ref = openModal<Sekcija>(this.dialog, {
      data: structuredClone(this.sekcija()),
      tool: { view: SectionComponent },
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
