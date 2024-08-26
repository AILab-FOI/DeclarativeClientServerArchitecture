import { Component, computed, input, signal } from '@angular/core';
import { Sekcija } from '../../core';
import { ContentComponent } from '../content/content.component';

@Component({
  selector: 'app-section',
  standalone: true,
  imports: [ContentComponent],
  templateUrl: './section.component.html',
  styleUrl: './section.component.scss',
})
export class SectionComponent {
  public sekcija = input.required<Sekcija>();
  public sadrzaj = computed(() => {
    return this.sekcija()?.sadrzaj;
  });

  public open = signal(true);
}
